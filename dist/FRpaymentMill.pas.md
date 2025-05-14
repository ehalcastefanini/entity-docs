<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa um componente de interface gráfica chamado `TFRAMEpaymentMill`, que é uma extensão de um frame base (`TFRAMEBaseGridEditSOA`). Ele é utilizado para gerenciar e exibir informações relacionadas a pagamentos de fábricas (mills) e integrações com sistemas SAP. O objetivo principal é fornecer uma interface para visualizar, editar e interagir com dados de pagamentos de fábricas, incluindo funcionalidades de busca e validação.

* **Tecnologias Utilizadas:**
  - **Delphi:** Linguagem de programação utilizada para criar a aplicação.
  - **Componentes cxGrid:** Para exibição de dados em formato de tabela.
  - **SOAP:** Para integração com serviços externos (MillServiceUtils e SapPaymentServiceUtils).
  - **DBClient:** Para manipulação de dados em datasets.

* **Forma do Componente:**
  - **Exibição em Grade (Grid Display):**
    - **Colunas da Grade e seus Tipos:**
      - `mill` (string): Código da fábrica.
      - `millPayment` (string): Código de pagamento da fábrica.
      - `millDescrip` (string): Descrição da fábrica.
      - `sapPayment` (string): Código de pagamento no SAP.
      - `stat` (string): Status.
    - **Ações da Grade e seus Efeitos:**
      - Edição de valores diretamente na grade.
      - Busca de valores utilizando botões de busca personalizados.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Adicionar novos registros.
  - Editar valores diretamente na grade.
  - Realizar buscas por código de fábrica ou código SAP.

* **Componentes Principais:**
  - `cxGridDBTableView`: Exibe os dados em formato de tabela.
  - `cxEditRepositoryItems`: Repositório de editores personalizados para campos específicos.
  - `AvailableActions`: Define as ações disponíveis, como "ADD" (adicionar).

* **Tradução para Pseudo-código:**
  - Evento `OnEditValueChanged`:
    ```pseudo
    se valor do campo na grade for alterado então
        validar e processar a alteração
    ```
  - Botão de busca para fábrica:
    ```pseudo
    se botão de busca for clicado então
        abrir diálogo de busca para selecionar fábrica
    ```
  - Botão de busca para SAP:
    ```pseudo
    se botão de busca for clicado então
        abrir diálogo de busca para selecionar código SAP
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do frame (`Create`):
     - Configurações de propriedades como `MasterKeyFields`, `DataPacketName` e `FrameType`.
     - Configuração da grade, incluindo campos ocultos, ordem de exibição e editores personalizados.
     - Atribuição de eventos para botões de busca.
  2. Interação do Usuário:
     - O usuário pode editar valores diretamente na grade.
     - O usuário pode clicar nos botões de busca para selecionar valores específicos.
  3. Funções Executadas:
     - `m_SetFindMill`: Configura busca para código de fábrica.
     - `m_SetFindSAP`: Configura busca para código SAP.
     - `m_InitializeData`: Inicializa os dados no dataset.

* **Dados Necessários:**
  - Código da fábrica (`mill`).
  - Código de pagamento da fábrica (`millPayment`).
  - Descrição da fábrica (`millDescrip`).
  - Código de pagamento no SAP (`sapPayment`).
  - Status (`stat`).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Ação "Adicionar" (`ADD`): Disponível por padrão.
  - Edição de valores: Permitida diretamente na grade.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas definidas no código.

* **Valores Padrão dos Campos:**
  - Não há valores padrão explícitos definidos no código.

* **Validações e Condições dos Campos:**
  - Campo `millPayment` e `millDescrip`: Validação para caracteres em maiúsculas.
  - Campo `sapPayment`: Validação para caracteres em maiúsculas e botão de busca.

---

## 5. Funções Principais:

* **`Create`:**
  - Configura as propriedades do frame e inicializa a grade.
* **`m_SetFindMill`:**
  - Configura a funcionalidade de busca para o código de fábrica.
* **`m_SetFindSAP`:**
  - Configura a funcionalidade de busca para o código SAP.
* **`m_InitializeData`:**
  - Inicializa os dados no dataset.

---

## 6. Consumo de Serviços API:

* **Serviços Externos:**
  - **MillServiceUtils:** Integração com serviços relacionados a fábricas.
  - **SapPaymentServiceUtils:** Integração com serviços relacionados a pagamentos SAP.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`: Para exibição de dados em formato de tabela.
  - `SOAPHTTPClient`: Para integração com serviços SOAP.
* **Componentes Customizados:**
  - `TFRAMEBaseGridEditSOA`: Frame base herdado.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `mill` (string, obrigatório): Código da fábrica.
  - `millPayment` (string, obrigatório): Código de pagamento da fábrica.
  - `millDescrip` (string, obrigatório): Descrição da fábrica.
  - `sapPayment` (string, obrigatório): Código de pagamento no SAP.
  - `stat` (string, obrigatório): Status.
* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `mill` → `mill`.
  - `millPayment` → `millPayment`.
  - `millDescrip` → `millDescrip`.
  - `sapPayment` → `sapPayment`.
  - `stat` → `stat`.

---

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  var
    Frame: TFRAMEpaymentMill;
  begin
    Frame := TFRAMEpaymentMill.Create(Self);
    Frame.Parent := Self;
  end;
  ```
* **HTML Representando a Grade:**
  ```html
  <table style="width: 100%; border: 1px solid black; border-collapse: collapse;">
    <thead>
      <tr>
        <th style="border: 1px solid black;">Mill</th>
        <th style="border: 1px solid black;">Mill Payment</th>
        <th style="border: 1px solid black;">Mill Description</th>
        <th style="border: 1px solid black;">SAP Payment</th>
        <th style="border: 1px solid black;">Status</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td style="border: 1px solid black;">001</td>
        <td style="border: 1px solid black;">PAY001</td>
        <td style="border: 1px solid black;">Factory A</td>
        <td style="border: 1px solid black;">SAP001</td>
        <td style="border: 1px solid black;">Active</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração de propriedades do frame no método `Create`.
* Configuração de eventos de busca para os campos `mill` e `sapPayment`.

---

## 12. Conclusão:

O código fornece uma interface robusta para gerenciar pagamentos de fábricas, com integração a sistemas externos. No entanto, faltam mensagens de erro e validações mais detalhadas. A modularidade e reutilização do frame são pontos fortes.

---

## 13. Resumo Curto:

O `TFRAMEpaymentMill` é um componente de interface gráfica para gerenciar pagamentos de fábricas, com funcionalidades de busca e integração com sistemas SAP. Ele utiliza uma grade para exibição e edição de dados, com suporte a validações e eventos personalizados.#### **FRpaymentMill.pas**

```
unit FRpaymentMill;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid;

type
  TFRAMEpaymentMill = class(TFRAMEBaseGridEditSOA)
    cxEDTupperCase: TcxEditRepositoryMaskItem;
    cxEDTfindSAP: TcxEditRepositoryButtonItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
  private
    FMillDescription: string;
    { Private declarations }
    procedure m_SetFindMill(Sender: TObject; AButtonIndex: Integer);
    procedure m_SetFindByCodeMill(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_InitializeData(DataSet: TDataSet);
    procedure SetMillDescription(const Value: string);
    procedure m_SetFindSAP(Sender: TObject; AButtonIndex: Integer);
    procedure m_SetFindByCodeSAP(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
    property MillDescription : string read FMillDescription write SetMillDescription; // guarda a descri��o em Portugu�s do c�digo fabril
  end;

var
  FRAMEpaymentMill: TFRAMEpaymentMill;

implementation

uses
  kneTypes, kneFindDialog, kneDialogFactory, kneUtils, kneFREditSOA, 
  kneFGFindUtils, Global,
  //---
  MillServiceUtils, SapPaymentServiceUtils;

{$R *.dfm}

{ TFRAMEpaymentMill }

constructor TFRAMEpaymentMill.Create(AOwner: TComponent);
begin
  inherited;

	// SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'paymentCode=gpsPayment';
  DataPacketName := 'LnkPayment';         // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'links';          // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  AvailableActions := 'ADD';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin

    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');

    // Ordem Campos ............................................................
    DefineOrderFields('mill;millPayment;millDescrip;sapPayment;stat');

    // Key Fields ..............................................................
    KeyFields:= 'mill;millPayment';

    // Custom Editors ..........................................................
    AddCustomField('mill', 'cxEDTfind');
    AddCustomField('stat', 'cxEDTstat');
    AddCustomField('millPayment', 'cxEDTupperCase');
    AddCustomField('millDescrip', 'cxEDTupperCase');
    AddCustomField('sapPayment', 'cxEDTfindSAP');      //JAR #10002  05-07-2011
//    AddCustomField('sapPayment', 'cxEDTupperCase');  //JAR #10002  05-07-2011
    ColsWidthInGrid := '60;100;200;100;100';
  end; //with
               
	// Atribui��o dos eventos dos Finds
  cxEDTfind.Properties.OnButtonClick := m_SetFindMill;
  cxEDTfindSAP.Properties.OnButtonClick := m_SetFindSAP;

end;

procedure TFRAMEpaymentMill.m_InitializeData(DataSet: TDataSet);
var
	lv_PayCode : string;
begin
```

#### **FRpaymentMill.dfm**

```
inherited FRAMEpaymentMill: TFRAMEpaymentMill
  ParentFont = True
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited cxSTLR: TcxStyleRepository
    inherited cxSTLReadOnly: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLDefault: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLInactive: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLgroupBox: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLheader: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLselection: TcxStyle
      Font.Name = 'Verdana'
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTupperCase: TcxEditRepositoryMaskItem
      Properties.CharCase = ecUpperCase
    end
    object cxEDTfindSAP: TcxEditRepositoryButtonItem
      Properties.Buttons = <
        item
          Default = True
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FF00FF4A667C
            BE9596FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FF6B9CC31E89E84B7AA3C89693FF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF4BB4FE51B5FF
            2089E94B7AA2C69592FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FF51B7FE51B3FF1D87E64E7AA0CA9792FF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            51B7FE4EB2FF1F89E64E7BA2B99497FF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF52B8FE4BB1FF2787D95F6A76FF
            00FFB0857FC09F94C09F96BC988EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FF55BDFFB5D6EDBF9D92BB9B8CE7DAC2FFFFE3FFFFE5FDFADAD8C3
            B3B58D85FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCEA795FD
            EEBEFFFFD8FFFFDAFFFFDBFFFFE6FFFFFBEADDDCAE837FFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFC1A091FBDCA8FEF7D0FFFFDBFFFFE3FFFFF8FFFF
            FDFFFFFDC6A99CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC1A091FEE3ACF1
            C491FCF2CAFFFFDDFFFFE4FFFFF7FFFFF7FFFFE9EEE5CBB9948CFF00FFFF00FF
            FF00FFFF00FFFF00FFC2A191FFE6AEEEB581F7DCAEFEFDD8FFFFDFFFFFE3FFFF
            E4FFFFE0F3ECD2BB968EFF00FFFF00FFFF00FFFF00FFFF00FFBC978CFBE7B7F4
            C791F2C994F8E5B9FEFCD8FFFFDDFFFFDCFFFFE0E2D2BAB68E86FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFD9C3A9FFFEE5F7DCB8F2C994F5D4A5FAE8BDFDF4
            C9FDFBD6B69089FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB58D85E8
            DEDDFFFEF2F9D8A3F4C48CF9D49FFDEAB8D0B49FB89086FF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFAD827FC9AA9EEFE0B7EFDFB2E7CEACB890
            86B89086FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFBA968ABB988CB79188FF00FFFF00FFFF00FFFF00FF}
          Kind = bkGlyph
        end>
      Properties.CharCase = ecUpperCase
      Properties.ClickKey = 114
    end
  end
end
```
<!-- tabs:end -->

