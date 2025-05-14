<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa um formulário para gerenciar e vincular entidades a partir de diferentes tipos e categorias. Ele permite que o usuário selecione tipos de entidades, insira informações relacionadas e utilize diálogos de busca para localizar entidades específicas. O objetivo é facilitar a manipulação e o gerenciamento de dados relacionados a entidades em um sistema.

* **Tecnologias Utilizadas:**
  - Delphi (VCL Framework).
  - Componentes visuais como `TsPanel`, `TsLabel`, `TcxDBImageComboBox`, `TsDBEdit`.
  - Serviços SOAP para comunicação com APIs externas.
  - Manipulação de datasets e banco de dados.

* **Forma do Componente:**
  - **Formulário:**
    - **Elementos do Formulário e seus Tipos:**
      - `TsLabel`: Rótulos para identificar campos.
      - `TcxDBImageComboBox`: Comboboxes para seleção de tipos de entidade.
      - `TsDBEdit`: Campos de edição vinculados ao banco de dados.
      - `TFRAMEFindEditSOA`: Componente para busca de entidades.
    - **Ações do Formulário e seus Efeitos:**
      - Alteração de valores nos comboboxes dispara eventos para manipulação de dados.
      - Campos de busca permitem localizar e vincular entidades específicas.

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Seleção de tipos de entidade.
  - Busca de entidades específicas através de diálogos de busca.
  - Manipulação de dados vinculados ao banco de dados.

* **Componentes Principais:**
  - `TFRAMEFindEditSOA`: Gerencia a busca de entidades.
  - `TcxDBImageComboBox`: Permite a seleção de tipos de entidade.
  - `TsDBEdit`: Exibe e edita informações de entidades.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` no combobox: `se combobox clicado então exibir opções`.
  - Evento `OnChange` no combobox: `se valor do combobox alterado então processar valor`.
  - Evento `OnExit` no combobox: `se combobox perder foco então validar entrada`.

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  - Inicialização do formulário: Configuração de propriedades e estilos.
  - Interação do usuário: Seleção de valores nos comboboxes e uso de diálogos de busca.
  - Processamento de dados: Manipulação de valores selecionados e validação.

* **Dados Necessários:**
  - Tipo de entidade.
  - Informações da entidade (URI, tipo, etc.).

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Comboboxes devem estar preenchidos para permitir a busca de entidades.
  - Campos obrigatórios devem ser preenchidos antes de salvar.

* **Filtros Disponíveis:**
  - Tipos de entidade.
  - Tipos de partes relacionadas (EbParty).

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se um campo obrigatório estiver vazio.
  - "Valor inválido" se um valor não for aceito.

* **Valores Padrão dos Campos:**
  - Não definidos explicitamente no código.

* **Validações e Condições dos Campos:**
  - Campos de texto devem aceitar apenas valores válidos.
  - Comboboxes devem conter opções válidas.

## 5. Funções Principais:

* **Descrição das Funções:**
  - `m_SetFindEntity`: Configura o componente de busca com base no tipo de entidade.
  - `m_PrepareFindDialogs`: Prepara os diálogos de busca.
  - `m_PrepareCombos`: Configura os comboboxes.
  - `m_DestroyFindDialogs`: Libera os recursos dos diálogos de busca.

## 6. Consumo de Serviços API:

* **Chamadas a Serviços Externos:**
  - Serviço: `EbEntityLinkServiceUtils`.
  - Finalidade: Gerenciar dados de entidades.
  - Dados enviados e recebidos não especificados no código.

## 7. Campos Condicionais (Lógica do Formulário):

* O campo de busca de entidade (`FRAMEFindEntity`) é exibido apenas quando um tipo de entidade é selecionado.

## 8. Dependências:

* **Bibliotecas Externas:**
  - `kneUtils`, `kneTypes`, `kneFGFindUtils`, `kneDialogFactory`, `kneFGGenericUtils`.
  - `DMskin` para estilos visuais.

* **Componentes Personalizados:**
  - `TFRAMEFindEditSOA`: Componente para busca de entidades.

## 9. Listagem de Campos e Validações:

* **Campos no Formulário:**
  - `Entity type` (tipo: combobox, obrigatório).
  - `Entity` (tipo: busca, obrigatório).
  - `EbEntity type` (tipo: combobox, obrigatório).
  - `EbParty Type` (tipo: combobox, obrigatório).
  - `Entity Uri` (tipo: texto, opcional).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não especificado no código.

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```pascal
  ICBOentity_tp.OnChange := ICBOentity_tpPropertiesEditValueChanged;
  ```
* **HTML Representando o Formulário:**
  ```html
  <div style="width: 616px; border: 1px solid #ccc; padding: 10px;">
    <label for="entityType">Entity type:</label>
    <select id="entityType" style="width: 100%;"></select>
    <label for="entity">Entity:</label>
    <input id="entity" type="text" style="width: 100%;" />
    <label for="ebEntityType">EbEntity type:</label>
    <select id="ebEntityType" style="width: 100%;"></select>
    <label for="ebPartyType">EbParty Type:</label>
    <select id="ebPartyType" style="width: 100%;"></select>
    <label for="entityUri">Entity Uri:</label>
    <input id="entityUri" type="text" style="width: 100%;" />
  </div>
  ```

## 11. Comentários Importantes no Código:

* Configuração inicial dos componentes no construtor `Create`.
* Liberação de recursos no destrutor `Destroy`.

## 12. Conclusão:

O código implementa um formulário robusto para gerenciar entidades, com suporte a busca e validação de dados. No entanto, faltam detalhes sobre validações específicas e integração com serviços externos.

## 13. Resumo Curto:

Formulário para gerenciar entidades, com suporte a busca, seleção de tipos e validação de dados, utilizando Delphi e serviços SOAP.#### **FRebEntityLink.pas**

```
unit FRebEntityLink;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, sFrameAdapter, ImgList,
  ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons,
  sBitBtn, sPanel, Mask, DBCtrls, sDBEdit, kneFRFindEditSOA, sLabel,
  kneFRStatusInfo, cxGraphics, cxControls, cxContainer, cxEdit, cxTextEdit,
  cxMaskEdit, cxDropDownEdit, cxImageComboBox, cxDBEdit, kneFindDialog, DMskin;

type
  TFRAMEebEntityLink = class(TFRAMEBaseCtrlEditSOA)
    PNL2: TsPanel;
    LBLeb_party_tp: TsLabel;
    LBLeb_Party: TsLabel;
    FRAMEFindEntity: TFRAMEFindEditSOA;
    EDTeb_Party: TsDBEdit;
    ICBOentity_tp: TcxDBImageComboBox;
    LBLentity_tp: TsLabel;
    LBLentity_cd: TsLabel;
    LBLeb_entity_type: TsLabel;
    ICBOeb_entity_type: TcxDBImageComboBox;
    ICBOeb_Party_Tp: TcxDBImageComboBox;
    LBLentityUri: TsLabel;
    EDTentityUri: TsDBEdit;
    procedure ICBOentity_tpPropertiesEditValueChanged(Sender: TObject);
    procedure CDStableAfterScroll(DataSet: TDataSet);
    procedure ICBOentity_tpExit(Sender: TObject);
    procedure ICBOentity_tpClick(Sender: TObject);
  private

    FindDlgAgent : TFORMkneFindDialog;
    FindDlgConsignee : TFORMkneFindDialog;
    FindDlgCustomer : TFORMkneFindDialog;
    procedure m_SetFindEntity(const p_EntityType: string);
    procedure m_PrepareFindDialogs;
    procedure m_PrepareCombos;
    procedure m_DestroyFindDialogs;
    procedure m_SetAccessMode(Sender: TObject; var pv_State: Boolean);
    procedure ProcessEntityTpCombo(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
    destructor Destroy; override;

  end;

var
  FRAMEebEntityLink: TFRAMEebEntityLink;

implementation

uses
  kneUtils, kneTypes, kneFGFindUtils, kneDialogFactory, kneFGGenericUtils
  //--- ServiceUtils
  , EbEntityLinkServiceUtils, AgentServiceUtils, kneFREditSOA;


{$R *.dfm}

constructor TFRAMEebEntityLink.Create(AOwner: TComponent);
begin
  inherited;

  // para ter certesa que os ponteiros apontam para nada
  // issue com o FastMM
  FindDlgCustomer := nil;
  FindDlgConsignee := nil;
  FindDlgAgent := nil;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := '';
  DataPacketName := 'EbEntityLink';        // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := '';         // nome do campo da metadata(entidadeMaster) que vai conter os details
  FrameType := frtMaster;

  ShowActionPanel := False;
  AvailableActions := '';
  PNLfooter.Visible := False;

  ProviderService := TEbEntityLinkServiceUtils.Create(Self);
  OnSetAccessMode := m_SetAccessMode;


  // Setup para os Finds
  m_PrepareFindDialogs;
  m_PrepareCombos;

  ICBOentity_tp.Style.StyleController := DMODskin.cxEditStyles1;
  ICBOeb_entity_type.Style.StyleController := DMODskin.cxEditStyles1;
  ICBOeb_Party_Tp.Style.StyleController := DMODskin.cxEditStyles1;
end;

destructor TFRAMEebEntityLink.Destroy;
begin
  m_DestroyFindDialogs;
  inherited;
```

#### **FRebEntityLink.dfm**

```
inherited FRAMEebEntityLink: TFRAMEebEntityLink
  Width = 616
  inherited PNLfooter: TsPanel
    Width = 616
  end
  object PNL2: TsPanel [1]
    Left = 0
    Top = 0
    Width = 616
    Height = 209
    Align = alTop
    TabOrder = 1
    SkinData.SkinSection = 'ALPHACOMBOBOX'
    object LBLeb_party_tp: TsLabel
      Left = 16
      Top = 112
      Width = 69
      Height = 13
      Caption = 'EbParty Type:'
      ParentFont = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5059883
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object LBLeb_Party: TsLabel
      Left = 16
      Top = 144
      Width = 42
      Height = 13
      Caption = 'EbParty:'
      FocusControl = EDTeb_Party
      ParentFont = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5059883
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object LBLentity_tp: TsLabel
      Left = 16
      Top = 16
      Width = 57
      Height = 13
      Caption = 'Entity type:'
      FocusControl = ICBOentity_tp
      ParentFont = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5059883
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object LBLentity_cd: TsLabel
      Left = 16
      Top = 48
      Width = 32
      Height = 13
      Caption = 'Entity:'
      FocusControl = FRAMEFindEntity
      ParentFont = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5059883
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object LBLeb_entity_type: TsLabel
      Left = 16
      Top = 80
      Width = 69
      Height = 13
      Caption = 'EbEntity type:'
      FocusControl = ICBOeb_entity_type
      ParentFont = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5059883
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object LBLentityUri: TsLabel
      Left = 16
      Top = 176
      Width = 48
      Height = 13
      Caption = 'Entity Uri:'
      FocusControl = EDTentityUri
      ParentFont = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5059883
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    inline FRAMEFindEntity: TFRAMEFindEditSOA
      Left = 107
      Top = 43
      Width = 486
```
<!-- tabs:end -->

