<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa uma interface para gerenciar uma lista de endereços e contatos associados. Ele permite visualizar, adicionar, modificar e excluir registros de endereços, além de integrar com um formulário de edição para manipular os detalhes de cada endereço. O objetivo principal é fornecer uma interface de usuário para gerenciar dados de endereços de forma eficiente.

* **Tecnologias Utilizadas:**
  - Delphi (VCL Framework).
  - Componentes de grade (cxGrid, cxGridDBTableView) para exibição de dados.
  - DataSets (TClientDataSet) para manipulação de dados.
  - SOAP (SOAPHTTPClient) para integração com serviços externos.
  - Componentes visuais personalizados (TsPanel, TsBitBtn).

* **Forma do Componente:**
  - **Exibição em Grade:**
    - **Colunas da Grade:**
      - Não especificado no código, mas presume-se que a grade exiba informações relacionadas a endereços, como "Nome", "Endereço", "Cidade", etc.
    - **Ações da Grade:**
      - Clique duplo em uma célula para abrir o formulário de edição.
      - Botão "Modificar" para editar o registro selecionado.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Visualizar registros de endereços em uma grade.
  - Adicionar novos endereços.
  - Modificar endereços existentes.
  - Integrar com um formulário de edição para manipular detalhes de endereços e contatos.

* **Componentes Principais:**
  - `TFRAMElistAddresses`: Frame principal que gerencia a exibição e manipulação de endereços.
  - `PNLmodify` e `BTNmodify`: Painel e botão para modificar registros.
  - `CDScontacts`: DataSet para gerenciar contatos associados.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão "Modificar": `se botão "Modificar" for clicado, então abrir formulário de edição`.
  - Evento `OnCellDblClick` da grade: `se célula for clicada duas vezes, então abrir formulário de edição`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  - Inicialização: O frame é criado e configurado com base nas propriedades do formulário pai.
  - Interação do Usuário:
    - O usuário pode clicar no botão "Modificar" ou dar um duplo clique em uma célula da grade para abrir o formulário de edição.
    - O formulário de edição utiliza os DataSets associados para manipular os dados.

* **Dados Necessários:**
  - Informações de endereços e contatos, como nome, endereço, cidade, etc.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - O botão "Modificar" só deve estar habilitado se um registro estiver selecionado na grade.

* **Filtros Disponíveis:**
  - Não especificado no código.

* **Mensagens de Erro:**
  - Não especificado no código, mas mensagens como "Nenhum registro selecionado" podem ser esperadas.

* **Valores Padrão dos Campos:**
  - Não especificado no código.

* **Validações e Condições dos Campos:**
  - Não especificado no código.

---

## 5. Funções Principais:

* **Funções e Lógica de Negócio:**
  - `CreateAndCallEditor`: Cria e chama o formulário de edição para manipular endereços.
  - `m_CheckPostalCodes`: Verifica os códigos postais dos endereços.
  - `m_ProcessEachRow`: Processa cada linha da grade.

---

## 6. Consumo de Serviços API:

* Não há chamadas explícitas a serviços externos no código fornecido.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais especificados no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`, `cxGridDBTableView`: Para exibição de dados em grade.
  - `SOAPHTTPClient`: Para integração com serviços SOAP.
  - `TsPanel`, `TsBitBtn`: Componentes visuais personalizados.

* **Componentes Personalizados:**
  - `TFRAMEBaseGridEditSOA`: Frame base herdado para funcionalidades de edição em grade.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - Não especificado no código.

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não especificado no código.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Trechos de Código:**
  - Exemplo de uso do botão "Modificar":
    ```delphi
    procedure TFRAMElistAddresses.BTNmodifyClick(Sender: TObject);
    begin
      CreateAndCallEditor(SelectedAddressID, SavePoint);
    end;
    ```
* **Capturas de Tela:** Não aplicável.

---

## 11. Comentários Importantes no Código:

* O frame utiliza dois DataSets: um para endereços (`CDStable`) e outro para contatos (`CDScontacts`).
* O formulário de edição (`TFORMMaddressAndContact`) é chamado para inserir, alterar ou visualizar endereços e seus detalhes.

---

## 12. Conclusão:

O código fornece uma interface robusta para gerenciar endereços e contatos associados. Ele é modular e reutilizável, permitindo integração com diferentes entidades. No entanto, faltam detalhes sobre validações de campos, mensagens de erro e filtros disponíveis, o que pode limitar sua usabilidade em cenários mais complexos.

---

## 13. Resumo Curto:

O `TFRAMElistAddresses` é um frame Delphi para gerenciar endereços e contatos, permitindo visualização, adição e edição de registros. Ele integra-se com um formulário de edição e utiliza DataSets para manipulação de dados.#### **FRlistAddresses.pas**

```
unit FRlistAddresses;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, kneUtils,
  MaddressAndContact;

type
  TFRAMElistAddresses = class(TFRAMEBaseGridEditSOA)
    PNLmodify: TsPanel;
    BTNmodify: TsBitBtn;
    procedure ACTaddExecute(Sender: TObject);
    procedure BTNmodifyClick(Sender: TObject);
    procedure cxDBVtableCellDblClick(Sender: TcxCustomGridTableView;
      ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
      AShift: TShiftState; var AHandled: Boolean);
  private
    FGetDataSet: TNotifyEvent;
    FformEditAddrs: TFORMMaddressAndContact;   // referencia para o form de edi��o de Address e Contracts
    { Private declarations }
    procedure SetGetDataSet(const Value: TNotifyEvent);
    procedure CreateAndCallEditor(pv_addrNum, pv_AddressSavePoint: Integer);
    procedure m_OnSetAccessMode(Sender: TObject; var pv_State: Boolean);
    procedure m_ProcessEachRow(pv_RowIndex: Integer;
      pv_RowInfo: TcxRowInfo);

  protected
    { protected declarations}
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
//    procedure LoadRecords(Sender: TObject);
//    function GetAddrNum: string;
//    function GetEntityType: string;
//    function GetEntityCode: string;

    procedure m_CheckPostalCodes;


  published
//    property entityCode: string read GetEntityCode;
//    property entityType: string read GetEntityType;
//    property addrNum: string read GetAddrNum;
//    function GetAddressMasterCDS: TClientDataSet;      // devolve uma referencia para o DS de Addresses
//    function GetAddressContactsCDS: TClientDataSet;    // devolve uma referencia para o DS de Contacts
    CDScontacts: TClientDataSet;  // referencia para o dataset de contacts da frame FRlistContacts
    property GetDataSet: TNotifyEvent read FGetDataSet write SetGetDataSet;  // Evento para obter a referencia do CDS de contacts
  end;

var
  FRAMElistAddresses: TFRAMElistAddresses;

implementation

uses
  kneTypes
  , Global {#24241};

{$R *.dfm}

{
// -----------------------------------------------------------------------------
//     Funcionamento da Frame e sua Estrutura
// -----------------------------------------------------------------------------
  Esta frame tem 2 dataset:
   1: CDStable -> Dataset default da frame (que tem os addresses)
   2: CDScontacts -> referencia para o DS da Frame(FRlistContacts)

  Esta frame chama o form (MaddressAndContact) sempre que seja para Inserir,
  alterar, ou visualizar um address e os seus detalhes.
  O form usa os datasets que existem nesta frame e o CDScontacts
  que � uma referencia para o da frame (FRlistContacts).
  Esta frame instancia as referencias no form para os CDS's.
  Para obter o ponteiro para o CDS da frame de contacts � utilizada um evento ()
  que dispara o pedido de preenchimento do pt. do dataset(CDScontacts) de Contacts
}

constructor TFRAMElistAddresses.Create(AOwner: TComponent);
var
  lv_DefaultAction: Boolean;
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  // As frames do address s�o utilizadas em diversas entidades
  //    (customer, consignee, carrier, agent, warehouse)
  // e em cada uma delas a chave de liga��o do detail (address) com a entidade
  // master � diferente. Para ultrapassar esta frame ao ser criada
  // consulta esta propriedade do form master e inicializa o masterKeyField
  // com o valor obtido, sen�o existir define o valor padr�o
  if TkneTypeInfo.fg_HasProperty(Owner, 'AddressMasterKeyFields') then
    MasterKeyFields := TkneTypeInfo.fg_GetStringPropertyValue(Owner, 'AddressMasterKeyFields')
  else
```

#### **FRlistAddresses.dfm**

```
inherited FRAMElistAddresses: TFRAMElistAddresses
  ParentFont = True
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnCellDblClick = cxDBVtableCellDblClick
      OptionsData.Deleting = False
      OptionsData.Editing = False
      OptionsData.Inserting = False
    end
  end
  inherited PNLfooter: TsPanel
    inherited PNLeditActions: TsPanel
      Left = 91
      Width = 318
      inherited PNLaddAction: TsPanel
        inherited BTNadd: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331310063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            63003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            310063319C003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630039181000FF00FF00FF00FF00FF00FF00FF00FF006331
            9C00315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00639CFF00315A
            E700315AE7003131CE003131630063313100FF00FF00FF00FF009C316300315A
            E700315AE700315AE700315AE7009C9CFF00FFFFFF00FFFFFF009C9CFF00315A
            E700315AE700315AE7003131CE0031313100FF00FF00FF00FF0063639C00315A
            E700315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00A5B5F700315A
            E700315AE700315AE700315AE70031319C0063313100FF00FF00315AE700315A
            E700639CFF006363FF00639CFF00A5B5F700FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00639CFF00315AE7003131CE0063310000FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE700315AE70063313100FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE7003131CE007B392100FF00FF00315AE7003163
            FF00A5B5F700A5B5F700A5B5F700CEEFF700FFFFFF00FFFFFF00CEEFF700A5B5
            F700A5B5F700A5B5F700315AE700315AE7007B392100FF00FF006363CE00315A
            E7006363FF006363FF00639CCE00A5B5F700FFFFFF00FFFFFF00A5B5F7003163
            FF003163CE00315AE700315AE70031319C009C5A3900FF00FF00CE636300315A
            E700639CFF00639CFF00639CFF00B5D6E700FFFFFF00FFFFFF00A5B5F7003163
            FF003163FF003163FF00315AE70063316300FF00FF00FF00FF00FF00FF006363
            9C00315AE700639CFF009C9CFF00CECEFF00FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00
            FF0063639C00315AE700639CFF00A5B5F700B5D6E700A5B5F700639CFF006363
            CE00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00CE6363006363CE00315AE7003163FF006363FF00315AE7006363
            CE009C636300CE633100FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLapplyAction: TsPanel
        inherited BTNapply: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF006331310063313100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF009C639C00A5B5F70031319C003131630031003100FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C316300F7F7F70063639C0000319C003131CE003131630063313100FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100639CCE006363CE0031319C00315AE700315AE70031319C0039181000FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF003131
            9C00315AE70031319C003163CE00315AE700315AE7003163CE00313163006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0063316300315A
            E70031319C0031639C00315AE700315AE700315AE7003163FF0031319C003131
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00315AE7003131
            CE0031319C00639CFF00639CFF00639CFF00639CFF009C9CFF003163CE003131
            630063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00639CCE006363
            CE009C9CCE00639CFF009C9CFF00639CFF00639CCE00A5B5F700A5B5F7003163
            CE003131310063313100FF00FF00FF00FF00FF00FF00FF00FF009C639C00CECE
            CE00A5B5F700CECEFF00A5B5F700A5B5F7006363CE009C9CCE00CECEFF00639C
            FF0031319C0031313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF009C9C
            9C009C9CCE009C9CCE00B5D6E70063639C00CE313100A5B5F7009C9CCE00CEEF
            F700639CFF003131630039181000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C639C009C639C009C316300FF00FF00FF00FF00CE636300CECECE009C9C
            CE00CECEFF003163CE003131630063313100FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE636300A5B5
            F7009C9CCE00CECEFF0031319C00313131007B392100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            6300639CCE009C9CFF00B5D6E70031319C0094422900FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE63630063639C006363CE009C9CCE00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLcancelAction: TsPanel
        inherited BTNcancel: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331000063313100633100006331
```
<!-- tabs:end -->

