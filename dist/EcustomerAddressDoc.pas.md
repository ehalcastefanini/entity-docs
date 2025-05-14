<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código apresentado implementa um formulário para gerenciar informações de endereços e documentação de clientes. Ele permite que os usuários visualizem, editem e organizem dados relacionados a endereços e contatos associados a entidades como clientes, consignatários, transportadoras, agentes e armazéns. O principal problema resolvido é a reutilização de frames de endereço em diferentes entidades, garantindo que a chave de ligação entre os detalhes (endereços) e a entidade principal seja configurada corretamente.

* **Tecnologias Utilizadas:**
  - Delphi (VCL e componentes visuais como `TsPageControl`, `TsSplitter`, `TsPanel`, etc.).
  - Componentes personalizados como `kneCBEdit`, `kneFREditSOA`, `kneFRGridEditSOA`.
  - Frames reutilizáveis para listas de contatos, endereços e informações de documentos.

* **Tipo de Formulário:**
  - **Formulário com Abas:**
    - **Elementos do Formulário:**
      - Aba "Address" (Endereço): Contém uma lista de endereços e contatos associados.
      - Aba "Documentation" (Documentação): Exibe informações relacionadas à documentação.
    - **Ações do Formulário:**
      - Visualizar, editar e organizar endereços e contatos.
      - Navegar entre abas para acessar diferentes tipos de informações.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Criar e inicializar o formulário com as configurações necessárias.
  - Configurar a ligação entre os endereços e a entidade principal.
  - Validar os dados inseridos antes de salvar.
  - Alternar entre abas para visualizar diferentes informações.

* **Componentes Principais:**
  - `PGCdetails`: Controle de abas para alternar entre "Address" e "Documentation".
  - `FRAMElistAddresses1`: Frame para exibir e gerenciar a lista de endereços.
  - `FRAMElistContacts1`: Frame para exibir e gerenciar a lista de contatos.
  - `FRAMEdocumentsInformation1`: Frame para exibir informações de documentos.

* **Pseudo-código de Ações e Eventos:**
  - Evento `OnCreate` do formulário: `Ao criar o formulário, inicializar propriedades e frames`.
  - Evento `OnShow` do formulário: `Ao exibir o formulário, carregar dados necessários`.
  - Função `m_Validate`: `Se os dados forem válidos, permitir salvar; caso contrário, exibir erro`.
  - Função `m_getData`: `Carregar dados do banco de dados para os frames`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do formulário (`FormCreate`): Configurações iniciais e definição de propriedades.
  2. Exibição do formulário (`FormShow`): Carregamento de dados e configuração de frames.
  3. Interação do usuário: Navegar entre abas, editar dados e salvar alterações.
  4. Validação e salvamento: Verificar se os dados são válidos antes de salvar.

* **Dados Necessários:**
  - Chave de ligação entre endereços e entidade principal.
  - Informações de endereços, contatos e documentos.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Ação: Salvar dados.
    - Pré-condição: Todos os campos obrigatórios devem estar preenchidos e válidos.
  - Ação: Editar endereço.
    - Pré-condição: Selecionar um endereço na lista.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - "Dados inválidos" se a validação falhar.
  - "Campo obrigatório não preenchido" se algum campo obrigatório estiver vazio.

* **Valores Padrão dos Campos:**
  - `AddressMasterKeyFields`: `'customer=entityCode;entityType'`.

* **Validações e Condições dos Campos:**
  - Não há validações explícitas definidas no código.

---

## 5. Funções Principais:

* **`Create`**: Inicializa o formulário e configura a propriedade `AddressMasterKeyFields`.
* **`m_CreateFormEdit`**: Cria uma instância do formulário para edição.
* **`m_FormEdit`**: Configura o formulário no modo de edição.
* **`m_getData`**: Carrega os dados necessários para os frames.
* **`m_Validate`**: Valida os dados antes de salvar.

---

## 6. Consumo de Serviços de API:

* Não há chamadas a serviços externos definidas no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `kneCBEdit`, `kneFREditSOA`, `kneFRGridEditSOA`: Componentes personalizados para edição e exibição de dados.
  - `TsPageControl`, `TsSplitter`, `TsPanel`: Componentes visuais para estruturação do formulário.

* **Componentes Personalizados:**
  - `FRAMElistAddresses`, `FRAMElistContacts`, `FRAMEdocumentsInformation`: Frames reutilizáveis para exibição de dados.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `AddressMasterKeyFields` (tipo: string, obrigatório): Define a chave de ligação entre endereços e entidade principal.

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não definido explicitamente no código.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:**  
  Não aplicável devido à ausência de lógica complexa.

* **Diagrama de Sequência:**  
  Não aplicável devido à ausência de interações com serviços externos.

* **Exemplo de Código:**
  ```delphi
  var
    Form: TFORMEcustomerAddressDoc;
  begin
    Form := TFORMEcustomerAddressDoc.Create(Application);
    try
      Form.ShowModal;
    finally
      Form.Free;
    end;
  end;
  ```

* **HTML Representando o Formulário:**
  ```html
  <div style="width: 878px; height: 620px; border: 1px solid #000; padding: 10px;">
    <h1>Customer Address and Documentation</h1>
    <div style="display: flex; flex-direction: column; height: 100%;">
      <div style="flex: 1; border-bottom: 1px solid #ccc;">
        <h2>Address</h2>
        <table style="width: 100%; border-collapse: collapse;">
          <tr>
            <th>Address</th>
            <th>City</th>
            <th>State</th>
          </tr>
          <tr>
            <td>123 Main St</td>
            <td>New York</td>
            <td>NY</td>
          </tr>
        </table>
      </div>
      <div style="flex: 1;">
        <h2>Contacts</h2>
        <p>Lista de contatos será exibida aqui.</p>
      </div>
    </div>
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* O frame de endereços é reutilizado em várias entidades, e a propriedade `AddressMasterKeyFields` é configurada para definir a chave de ligação.

---

## 12. Conclusão:

O código implementa um formulário robusto para gerenciar endereços e documentação de clientes, com reutilização eficiente de frames. No entanto, faltam validações explícitas e mensagens de erro detalhadas, o que pode ser melhorado.

---

## 13. Resumo Curto:

Formulário Delphi para gerenciar endereços e documentação de clientes, com reutilização de frames e configuração dinâmica de chaves de ligação.#### **EcustomerAddressDoc.pas**

```
unit EcustomerAddressDoc;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBEdit, ActnList, ImgList, knePrivileges, StdCtrls, Buttons,
  sBitBtn, sSpeedButton, ToolWin, ComCtrls, acCoolBar, ExtCtrls, sPanel,
  kneEnterAsTab, sPageControl, kneFREditSOA, kneFRCtrlEditSOA,
  FRcustomerAddressDoc, sSplitter, FRlistContacts, FRlistAddresses,
  kneFRGridEditSOA, FRdocumentsInformation, cxControls, cxSplitter;

type
  TFORMEcustomerAddressDoc = class(TFORMkneBaseEdit)
    PGCdetails: TsPageControl;
    TSHaddress: TsTabSheet;
    TSHdocumentation: TsTabSheet;
    PNLmaster: TsPanel;
    FRAMEdocumentsInformation1: TFRAMEdocumentsInformation;
    FRAMElistAddresses1: TFRAMElistAddresses;
    SPL2: TsSplitter;
    FRAMElistContacts1: TFRAMElistContacts;
    FRAMEcustomerAddressDoc1: TFRAMEcustomerAddressDoc;
    SPL1: TsSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FAddressMasterKeyFields: String;   // � necess�ria para definir a liga��o com os address ( uma vez que estes s�o utilizados por varias entidades)
    function GetAddrMasterKeyFields: String;
    procedure m_SetContactsDataSet(sender: TObject);

  protected
    procedure m_getData; override;
    function m_Validate: Boolean; override;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent);

    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
    procedure m_FormEdit(const pv_AccessMode: string; const pv_KeyValues: string = '');  override;

  published
    property AddressMasterKeyFields: string read GetAddrMasterKeyFields; // � necess�ria para definir a liga��o com os address ( uma vez que estes s�o utilizados por varias entidades)
  end;

var
  FORMEcustomerAddressDoc: TFORMEcustomerAddressDoc;

implementation

uses
  kneUtils, Global;
  
{$R *.dfm}

{ TFORMEcustomerAddressDoc }


constructor TFORMEcustomerAddressDoc.Create(AOwner: TComponent);
begin
  // As frames do address s�o utilizadas em diversas entidades
  //    (customer, consignee, carrier, agent, warehouse)
  // e em cada uma delas a chave de liga��o do detail (address) com a entidade
  // master � diferente. Para ultrapassar isso a frame Address ao ser criada
  // consulta esta propriedade do form master e inicializa o masterKeyField dela
  // com este valor
  FAddressMasterKeyFields := 'customer=entityCode;entityType';
  
  inherited;

  PGCdetails.ActivePageIndex := 0;
end;      

procedure TFORMEcustomerAddressDoc.FormCreate(Sender: TObject);
begin
  inherited;
end;


// ################ FormCreate  ######################################
class function TFORMEcustomerAddressDoc.m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit;
begin
  // Substituir pelo nome do form
  Result := TFORMEcustomerAddressDoc.Create(Application);
end;

procedure TFORMEcustomerAddressDoc.m_FormEdit(const pv_AccessMode: string;   //JAR #5298  05-02-2010
      const pv_KeyValues: string = '');
begin
  StringAccessMode := 'MODIFY';
  inherited;
end;

procedure TFORMEcustomerAddressDoc.m_getData;
var
  lv_MasterFrame: TFRAMEBaseEditSOA;
begin
  Screen.Cursor := crHourGlass;
```

#### **EcustomerAddressDoc.dfm**

```
inherited FORMEcustomerAddressDoc: TFORMEcustomerAddressDoc
  Left = 300
  Top = 100
  Width = 878
  Height = 620
  Caption = 'Customer Address and Documentation'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object SPL1: TsSplitter [1]
    Left = 0
    Top = 191
    Width = 870
    Height = 4
    Cursor = crVSplit
    Align = alTop
    Color = clBtnFace
    ParentColor = False
    SkinData.SkinSection = 'FORM'
  end
  inherited PNLtoolbar: TsPanel
    Width = 870
    inherited CLBactions: TsCoolBar
      Width = 870
      Bands = <
        item
          Control = PNbotoes
          ImageIndex = -1
          MinHeight = 41
          Width = 866
        end>
      inherited PNbotoes: TsPanel
        Width = 853
        inherited PNnew: TsPanel
          Visible = False
        end
      end
    end
  end
  object PGCdetails: TsPageControl [3]
    Left = 0
    Top = 195
    Width = 870
    Height = 398
    ActivePage = TSHaddress
    Align = alClient
    TabOrder = 1
    SkinData.SkinSection = 'PANEL'
    object TSHaddress: TsTabSheet
      Caption = '&Address'
      SkinData.CustomColor = False
      SkinData.CustomFont = False
      object SPL2: TsSplitter
        Left = 0
        Top = 100
        Width = 862
        Height = 4
        Cursor = crVSplit
        Align = alTop
        Color = clBtnFace
        ParentColor = False
        SkinData.SkinSection = 'FORM'
      end
      inline FRAMElistAddresses1: TFRAMElistAddresses
        Left = 0
        Top = 0
        Width = 862
        Height = 100
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentBackground = False
        ParentFont = False
        TabOrder = 0
        inherited cxDBG: TcxGrid
          Width = 862
          Height = 66
        end
        inherited PNLfooter: TsPanel
          Top = 66
          Width = 862
          inherited PNLmodify: TsPanel
            inherited BTNmodify: TsBitBtn
              Left = 1
            end
          end
        end
      end
      inline FRAMElistContacts1: TFRAMElistContacts
        Left = 0
        Top = 104
        Width = 862
        Height = 266
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
```
<!-- tabs:end -->

